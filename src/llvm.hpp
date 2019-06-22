#ifndef ILANG_LLVM_HPP
#define ILANG_LLVM_HPP 1

#include <optional>

#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/IRBuilder.h"

#include "ilang/Expr.hpp"

namespace ilang{
	llvm::Type *getLlvmIntType(llvm::LLVMContext &ctx, const TypeData &typeData, TypeHandle type);
	llvm::Type *getLlvmType(llvm::LLVMContext &ctx, const TypeData &typeData, TypeHandle type);
	
	class LLVMModule{
		public:
			explicit LLVMModule(llvm::LLVMContext *context_)
				: context(context_){}
			
			llvm::ConstantInt *compileIntLiteral(const Exprs::IntLiteral *intLit);
			llvm::ConstantFP *compileRealLiteral(const Exprs::RealLiteral *intLit);
			llvm::Value *compileLiteralExpr(const Exprs::Literal *lit);
			
			llvm::Value *compileBinOpExpr(const Exprs::BinOp *binOp);
			
			llvm::Value *compileExpr(ExprHandle expr);
			
		private:
			llvm::LLVMContext *context;
			std::unique_ptr<llvm::Module> module;
			
			friend class LLVMCompiler;
	};
	
	class LLVMCompiler{
		private:
			llvm::orc::ExecutionSession executionSession;
			llvm::orc::RTDyldObjectLinkingLayer objectLayer;
			llvm::orc::IRCompileLayer compileLayer;

			llvm::DataLayout dataLayout;
			llvm::orc::MangleAndInterner mangle;
			llvm::orc::ThreadSafeContext context;
			
			
		public:
			LLVMCompiler(
				llvm::orc::JITTargetMachineBuilder targetBuilder,
				llvm::DataLayout dataLayout_
			)
				: objectLayer(executionSession, []{ return std::make_unique<llvm::SectionMemoryManager>(); })
				, compileLayer(
					executionSession,
					objectLayer,
					llvm::orc::ConcurrentIRCompiler(std::move(targetBuilder))
				  )
				, dataLayout(std::move(dataLayout_))
				, mangle(executionSession, dataLayout)
				, context(llvm::make_unique<llvm::LLVMContext>())
			{
				
				executionSession.getMainJITDylib().setGenerator(
					llvm::cantFail(llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(dataLayout))
				);
			}
			
			std::optional<llvm::JITEvaluatedSymbol> lookup(llvm::StringRef name){
				auto res = executionSession.lookup({&executionSession.getMainJITDylib()}, mangle(name.str()));
				return res ? std::make_optional(std::move(*res)) : std::nullopt;
			}
	};
	
	std::unique_ptr<LLVMCompiler> createJITCompiler(){
		auto targetBuilder = llvm::orc::JITTargetMachineBuilder::detectHost();
		
		if(!targetBuilder){
			// throw error
			
			return nullptr;
		}
		
		auto dataLayout = targetBuilder->getDefaultDataLayoutForTarget();
		if(!dataLayout){
			// throw error
			return nullptr;
		}
		
		return std::make_unique<LLVMCompiler>(std::move(*targetBuilder), std::move(*dataLayout));
	}
}

#endif // !ILANG_LLVM_HPP
